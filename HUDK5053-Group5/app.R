#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(markdown)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(syuzhet)
library(ggplot2)
library(sjPlot)

# UI ------------------------
### Dashboard Sidebar
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    )
)

### Dashboard Body
body <- dashboardBody(
    tabItems(
        ## using box to display plots
        tabItem(tabName = "dashboard",
                fluidRow(
                    box(title = "General Information", status = "primary", width = 6, 
                        "Engagement rate: 64%", br(), 
                        "Total correct answers: 23.44%", br(),
                        "Average score: 683.94 points", br(),
                        plotOutput("plotall", height = 344)),
                    tabBox(id = "tabchart1", width = 6,
                           tabPanel("Quiz1", plotOutput("plot1")),
                           tabPanel("Quiz2", plotOutput("plot2")),
                           tabPanel("Quiz3", plotOutput("plot3")),
                           tabPanel("Quiz4", plotOutput("plot4")),
                           tabPanel("Comparison", plotOutput("comp")),
                           tabPanel("Correlation", plotOutput("corr"))
                    )
                    
                ),
                fluidRow(
                    tabBox(id = "tabchart2", title = "Overall Satisfaction", width = 6,
                           tabPanel("Frequency", plotOutput("sentiment.count")),
                           tabPanel("Percentage", plotOutput("sentiment.perc"))
                    ),
                    tabBox(title = "Survey Feedback: Likert-scales", width = 6, selected = "Plot",
                           tabPanel("Questions", htmlOutput("q.list")), 
                           tabPanel("Plot", plotOutput("likert.scale")))
                )
        )
    )
)

ui <- dashboardPage(
    dashboardHeader(title = "HUDK5053 Group 5"),
    sidebar,
    body
)

# Server------------------------------------
server <- function(input, output, session) {
    output$menuitem <- renderMenu({
        menuItem("Menu item", icon = icon("calendar"))
    })
    
    ### Performance ###
    # load quiz data
    quiz_scores <- read.csv("quiz_scores.csv", header = TRUE)
    quiz_scores$total_answertime <- quiz_scores$quiz1_answertime + quiz_scores$quiz2_answertime + 
        quiz_scores$quiz3_answertime + quiz_scores$quiz4_answertime # new variable: total answer time
    
    # Plot Histogram: final score
    output$plotall <- renderPlot({
        hist(quiz_scores$final_score, 
             breaks = 5, 
             main = "Quiz Score Distribution", 
             xlab = "Final Score")
    })
    
    # Plot Histogram: quiz 1
    output$plot1 <- renderPlot({
        hist(quiz_scores$quiz1_score, 
             main = "Quiz 1 Score Distribution", 
             xlab = "Score")
    })
    
    # Plot Histogram: quiz 2
    output$plot2 <- renderPlot({
        hist(quiz_scores$quiz2_score, 
             main = "Quiz 2 Score Distribution", 
             xlab = "Score")
    })
    
    # Plot Histogram: quiz 3
    output$plot3 <- renderPlot({
        hist(quiz_scores$quiz3_score, 
             main = "Quiz 3 Score Distribution", 
             xlab = "Score")
    })
    
    # Plot Histogram: quiz 4
    output$plot4 <- renderPlot({
        hist(quiz_scores$quiz4_score, 
             main = "Quiz 4 Score Distribution", 
             xlab = "Score")
    })
    
    # Comparison
    output$comp <- renderPlot({
        # create mean score
        quiz1_mean <- mean(quiz_scores$quiz1_score)
        quiz2_mean <- mean(quiz_scores$quiz2_score)
        quiz3_mean <- mean(quiz_scores$quiz3_score)
        quiz4_mean <- mean(quiz_scores$quiz4_score)
        mean_score <- c(quiz1_mean,quiz2_mean,quiz3_mean,quiz4_mean)
        quiz <- c("quiz1","quiz2","quiz3","quiz4")
        data <- data.frame(quiz = quiz, mean_score = mean_score)
        
        ggplot(data) +
            geom_col(aes(x = quiz, y = mean_score)) +
            ggtitle("Comparison of Mean Score across Quizzes") +
            xlab("Quiz") +
            ylab("Average score")
    })
    
    # Correlation between Final Score and Answer Time
    output$corr <- renderPlot({
        plot(x = quiz_scores$total_answertime, y = quiz_scores$final_score, 
             main = "Correlation between Final Score and Answer Time",
             xlab = "Total Answer Time (Seconds)",
             ylab = "Final Score")
        abline(lm(final_score ~ total_answertime, data = quiz_scores))
    })
    
    
    ### Feedback ###
    # Read the text file
    feedback_text <- readLines("feedback_text.txt")
    
    # Load the data as a corpus
    corpus <- Corpus(VectorSource(feedback_text))
    
    # Clean text data
    #Remove spaces
    corpus <- tm_map(corpus, stripWhitespace)
    #Convert to lower case 
    corpus <- tm_map(corpus, tolower) 
    #Remove pre-defined stop words ('the', 'a', etc) 
    corpus <- tm_map(corpus, removeWords, stopwords('english'))
    #Remove punctuation 
    corpus <- tm_map(corpus, removePunctuation)
    #Convert words to stems ("education" = "edu") for analysis
    corpus <- tm_map(corpus, stemDocument)
    # Remove your own stop word
    corpus <- tm_map(corpus, removeWords, c("class", "today", "susan", "xinxin"))
    
    # view the corpus
    corpus[[1]][1]
    
    # Build a term-document matrix
    text_dtm <- TermDocumentMatrix(corpus)
    dtm_m <- as.matrix(text_dtm)
    # Sort by descearing value of frequency
    dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
    dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
    # Display the top 5 most frequent words
    head(dtm_d, 5)
    
    ## Sentiment scores
    # regular sentiment score using get_sentiment() function and method of your choice
    # please note that different methods may have different scales
    syuzhet_vector <- get_sentiment(feedback_text, method="syuzhet")
    # see the first row of the vector
    head(syuzhet_vector)
    # see summary statistics of the vector
    summary(syuzhet_vector)
    
    # Emotion Classification
    # run nrc sentiment analysis to return data frame with each row classified as one of the following
    # emotions, rather than a score: 
    # anger, anticipation, disgust, fear, joy, sadness, surprise, trust 
    # It also counts the number of positive and negative emotions found in each row
    d <- get_nrc_sentiment(feedback_text)
    # head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
    head (d,10)
    
    # Visualize emotions
    #transpose
    td <- data.frame(t(d))
    #The function rowSums computes column sums across rows for each level of a grouping variable.
    td_new <- data.frame(rowSums(td[2:7]))
    #Transformation and cleaning
    names(td_new)[1] <- "count"
    td_new <- cbind("sentiment" = rownames(td_new), td_new)
    rownames(td_new) <- NULL
    td_new2<-td_new[1:8,]
    
    #Plot One - count of words associated with each sentiment
    output$sentiment.count <- renderPlot({
        quickplot(sentiment, data = td_new2, weight = count, geom = "bar", fill = sentiment, 
                  ylab = "Count", xlab = "Sentiments") + 
            ggtitle("Sentiments in Survey Feedback (as frequency)") +
            labs(fill = "Sentiments") +
            theme_bw()
    })
    
    #Plot two - count of words associated with each sentiment, expressed as %
    output$sentiment.perc <- renderPlot({
        prop_tb <- data.frame(colSums(prop.table(d[, 1:8])))
        prop_value <- prop_tb[,1]
        prop_name <- rownames(prop_tb)
        prop_tb <- data.frame(Sentiments = prop_name, Percentage = prop_value)
        
        # barplot
        ggplot(prop_tb, aes(x = reorder(Sentiments, Percentage), y = Percentage, fill = Sentiments)) +
            ggtitle("Sentiments in Survey Feedback (as percentage)") +
            xlab("Sentiments") +
            scale_y_continuous(labels = scales::percent) +
            geom_bar(stat = "identity") +
            coord_flip() +
            theme_bw()
    })
    
    ## Likert Scales
    # load data
    class_feedback <- read.csv("feedback_likert.csv", header = TRUE)
    class_feedback$Q1 <- as.factor(class_feedback$Q1)
    class_feedback$Q2 <- as.factor(class_feedback$Q2)
    class_feedback$Q3 <- as.factor(class_feedback$Q3)
    class_feedback$Q4 <- as.factor(class_feedback$Q4)
    class_feedback$Q5 <- as.factor(class_feedback$Q5)
    class_feedback$Q6 <- as.factor(class_feedback$Q6)
    class_feedback$Q7 <- as.factor(class_feedback$Q7)
    class_feedback$Q8 <- as.factor(class_feedback$Q8)
    
    # create likert plot
    output$likert.scale <- renderPlot({
        likert_6 <- class_feedback
        levels_6 <- c("Strongly agree", "Agree", "Somewhat agree", 
                      "Somewhat disagree", "Disagree", "Strongly disagree")
        
        plot_likert(likert_6, 
                    legend.labels = levels_6, 
                    reverse.colors = TRUE, 
                    values = "sum.outside", 
                    title = "Questionnaire Feedback",
                    geom.size = 0.8,
                    show.prc.sign = TRUE,
                    grid.range = c(0.8,1.2))
    })
    
    # survey questions
    output$q.list <- renderUI({
        ques.list <- c("Q1: I have background knowledge of conditional probability.",
                       "Q2: I like the topic that group 5 taught us today.",
                       "Q3: I think the conditional probability is useful in my career development.",
                       "Q4: I think the conditional probability is useful in my academic development.",
                       "Q5: I think today's quizzes are difficult.",
                       "Q6: Overall, group 5 designed the mini-class very well.",
                       "Q7: Group 5 members taught the concept clearly.",
                       "Q8: The in-class activities are interesting.")
        HTML(paste(ques.list, sep = "", collapse = '<br/>'))
    })
    
}


# Run the application 
shinyApp(ui, server)

library(shiny)
library(quanteda)
library(data.table)

navbarPage(
        "Typing Helper",
        tabPanel("Application",
                 p("Hello, I'm a typing helper."),
                 p("Write anything below and I'll suggest the next words for you!"),
                 textAreaInput('text.input', label = "", value = "", width = 500, height = 50),
                 actionButton("sug1", "", width = 160),
                 actionButton("sug2", "", width = 160),
                 actionButton("sug3", "", width = 160)
        ),
        tabPanel("About this app", 
                 strong(p("The aim of this project was to put to practice all knowledge developed in the Coursera Data Science Specialisation by making an application that is able to suggest the next word to the user.")),
                 hr(),
                 h4("How the app works"),
                 p("1. The learning process is done through the processing of a large txt dataset into ngrams."),
                 p("2. The text input by the user is also processed and then searched into the ngrams lists. The 3 most frequent pentagrams that 'start' with the words input by the user are returned. Their last words are displayed as suggestions."),
                 p("3. If no pentagrams are found, then a stupid backoff model is applied. That means that the application will try to search for fitting quadgrams, still, if no quadgrams are found, trigrams will be tested."),
                 p("4. If in all levels no ngrams are found, the app returns a message asking the user to check for spelling errors.")
        ),
        tabPanel("About me",
                 img(src='download.png', align = "left", width = 150), h1("Danilo Dias Araujo"),
                 hr(),
                 strong(p("A London based and results driven data analyst with strong commercial skills and business acumen. Willing to work with data science in business environments to solve real-life problems. A professional with a background in engineering and a track record of delivering projects to budget within deadlines and effective use of resources.")),
                 strong("Technical Skill Set:"),
                 h5("- R programming"),
                 h5("- SQL"),
                 h5("- Practical Machine Learning"),
                 h5("- Data Cleaning"),
                 hr(),
                 h3(a("Portfolio", href = "https://github.com/danilodaraujo/PORTFOLIO-DS/blob/master/README.md")), h3(a("Linkedin", href = "https://www.linkedin.com/in/danilodaraujo/"))
        )
)

library(shiny)

ui <- fluidPage(
  mainPanel(
    tabsetPanel(
      tabPanel("Live",
               h4(textOutput("event"), style="font-weight: bold"),
               h5(textOutput("location")),
               h5(span(textOutput("bout"), style="color:#4FB06D; font-weight: bold")),
               textOutput("pred"),
               
               p("Red corner:",
                 span(textOutput("fighter_r", inline = TRUE), style = "color:red"),
                 style="font-weight: bold"),
               
               p("Blue corner:",
                 span(textOutput("fighter_b", inline = TRUE), style = "color:blue"),
                 style="font-weight: bold"),
               
               p("GaMMA's prediction of the judges' scorecard:", style="font-weight: bold"),
               
               fluidRow(
                 column(12,
                        tableOutput("scores_table")
                 )
               ),
               
               span("GaMMA's prediction that",
                    span(textOutput("text2", inline = TRUE), style = "color:red"),
                    "wins the fight:", style="font-weight: bold"),
               
               fluidRow(
                 column(12,
                        tableOutput("pred_r_table")
                 )
               ),
      ), 
      
      tabPanel("About", 
               h4("GaMMA: Real-time analytics for the sport of MMA", style="font-weight: bold"),
               h4("Overview", style="font-weight: bold"),
               p("GaMMA delivers ",
                 strong("real-time analytics for UFC fights"), 
                 " based on the data provided by ",
                 a("ESPN.",
                   href = "https://www.espn.com/mma/fightcenter")),
               p("In short, we used the data provided by the ",
                 a("UFC",
                   href = "http://www.ufcstats.com/statistics/events/completed"),
                 "and ",
                 a("MMA Decisions",
                   href = "http://www.mmadecisions.com/"),
                 "to build predictive models and we take advantage of the data provided 
                 in real-time mode by ESPN to deliver real-time analytics for UFC fights."),
               p("GaMMA provides two predictions in real-time both with 80% accuracy:"),
               p("- the judges' majority score as soon as a round is over"),
               p("- the prediction that the red fighter wins the fight in 3-round fights that go 
                 beyond the first two rounds (53% of all UFC fights)"),
               p("For more information about GaMMA, please refer to the preprint and ",
                 a("github.",
                   href = "https://github.com/vincentberthet/gamma"),
                 "Follow us on ",
                 a("Twitter.", 
                   href = "https://twitter.com/gamma_app")),
               h4("Bios", style="font-weight: bold"),
               p(a("Vincent Berthet",
                   href = "https://twitter.com/vincentberthet"),
                 " is the founder and project leader of GaMMA. Vincent is an Associate Professor in 
               cognitive science at the University of Lorraine in France and associate researcher 
               at the Centre d’Economie de la Sorbonne. He holds a PhD in cognitive science and 
               a MSc in political science from Paris 1 Pantheon-Sorbonne University. 
               Vincent has been practicing martial arts for over 30 years and he has been 
               passionate about MMA since 1997 when he watched UFC 2."),
               p(a("Romain Maillard",
                   href = "https://github.com/romainm13"),
                 " is the CDO of GaMMA. He is responsible for the data gathering process using 
                 web scraping techniques. As a French engineering student from Mines Nancy 
                 with a specialization in big data and AI, Romain is passionate about computer 
                 science, automation, and the potential of AI. Romain enjoys staying up-to-date 
                 on the latest advancements in tech.")
      ),
    )
  )
)

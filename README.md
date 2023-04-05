# v1
First version of GaMMA

GaMMA1 delivers real-time analytics for UFC fights based on the data provided by ESPN. GaMMA1 provides two predictions both with 80% accuracy:

1/ the judges' majority score as soon as a round is over

2/ the probability that the red fighter wins the fight in 3-round fights that go beyond the first two rounds (53% of all UFC fights)

GaMMA1 works as follows. We first conducted regression analyses on the data provided by the UFC and MMA Decisions to build predictive models of UFC fight outcomes. Then, we took advantage of these models and the data provided in real-time mode by ESPN to build an R shiny app that delivers real-time predictions for UFC fights. Every fight night, ESPN provides in real-time mode the cumulative data for nine key fight statistics. We designed a Python program that scraps every five seconds the ESPN live data of the on-going fight.

\name{visualize_airport_delays}
\alias{visualize_airport_delays}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
airprt
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
visualize_airport_delays()
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function () 
{
    subset_f <- select(flights, flight, origin, dest, dep_delay, 
        arr_delay)
    subset_a <- select(airports, faa, name, lat, lon)
    new_flights <- mutate(subset_f, total_delay = dep_delay + 
        arr_delay)
    temp <- inner_join(new_flights, subset_a, by = c(dest = "faa"))
    by_name <- group_by(temp, name)
    delay <- summarize(by_name, m_delay = mean(total_delay, na.rm = TRUE))
    data <- inner_join(subset_a, delay, by = "name")
    graph <- ggplot(data = data, aes(x = lon, y = lat)) + geom_point(aes(color = m_delay)) + 
        xlab("Longitude") + ylab("Latitude") + ggtitle("The mean delay of flights for different airports") + 
        scale_color_gradient(low = "blue", high = "red")
    return(graph)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

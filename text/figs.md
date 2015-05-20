# Figures

\begin{figure}[htbp]
\begin{center}
\includegraphics[width=4in]{../figs/motivate.pdf}
\caption{Assessment methods can suggest conflicting population statuses. Example
trajectories of \bbmsy\\ estimated by four data-limited assessment methods
(colours) and a TODO data-rich stock assessment (black). Lines indicate median
fits and shaded regions interquartile ranges.}
\label{fig:didactic}
\end{center}
\end{figure}

\clearpage

\begin{figure}[htbp]
\begin{center}
\includegraphics[width=0.9\textwidth]{../figs/didactic.pdf}
\caption{Using an ensemble model to predict population status.
Individual models are fit to a dataset with a number of populations with a good
estimate of population status. These estimates, along with potential additional
covariates, are then used as coviarates in an ensemble model fitted to known
population status. Finally, the same individual models are fit to a population
of interest and combined with the fitted ensemble model to derive the ensemble
prediction of population status.}
\label{fig:didactic}
\end{center}
\end{figure}

\clearpage

\begin{figure}[htbp]
\begin{center}
\includegraphics[width=\textwidth]{../figs/fig2.pdf}
\caption{True vs. predicted \bbmsy\\ for a dataset of simulated fisheries
dynamics. Upper panels represent four individual data-limited methods. The
output from these methods is combined, along with additional covariates, to
form the ensemble model estimates along the lower row. These results are
derived from a repeated three-fold cross-validation procedure. The data were
binned into hexagons for visual presentation. Darker areas indicate areas with
greater density of points.}
\label{fig:sim-hexagon}
\end{center}
\end{figure}

\clearpage

\begin{figure}[htbp]
\begin{center}
\includegraphics[width=3.7in]{../figs/fig3.pdf}
\caption{Performance of individual and ensemble models fitted to a simulated
dataset. The x-axis represents within-population innaccuracy---specifically
median absolute relative error. The y-axis represents across-population
Spearman rank-order correlation. The colour shading represents bias in relative
error: white points are unbiased, blue points methods that predict \bbmsy\\
values that are too high, red points represent methods that predict \bbmsy\\
values that are too low. The top-left corner contains methods with the best
performance across the two metrics.}
\label{fig:performance-sim}
\end{center}
\end{figure}

<!--
\begin{figure}[htbp]
\begin{center}
\includegraphics[width=3.7in]{../figs/performance-beanplots-sim.pdf}
\caption{Performance with simulation ensembles (alternate of previous figure).}
\label{fig:performance-sim}
\end{center}
\end{figure}
-->
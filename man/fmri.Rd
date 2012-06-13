\name{fmri}
\alias{fmri}
\docType{data}
\title{fMRI
%%   ~~ data name/kind ... ~~
}
\description{Data (as a vector list) from an fMRI experiment in pain, listed  by location and stimulus.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(fmri)}
%%\format{
%%  The format is:
%%List of 54
%% $ L1T1: num [1:128, 1:5] \cr
%% $ L1T2: num [1:128, 1:4] \cr
%% $ L1T3: num [1:128, 1:5] \cr
%% $ L1T4: num [1:128, 1:3] \cr
%% $ L1T5: num [1:128, 1:5] \cr
%% $ L1T6: num [1:128, 1:4] \cr
%% $ L2T1: num [1:128, 1:5] \cr
%% $ L2T2: num [1:128, 1:4] \cr
%% $ L2T3: num [1:128, 1:5] \cr
%% $ L2T4: num [1:128, 1:3] \cr
%% $ L2T5: num [1:128, 1:5] \cr
%% $ L2T6: num [1:128, 1:4] \cr
%% $ L3T1: num [1:128, 1:5] \cr
%% $ L3T2: num [1:128, 1:4] \cr
%% $ L3T3: num [1:128, 1:5] \cr
%% $ L3T4: num [1:128, 1:3] \cr
%% $ L3T5: num [1:128, 1:5] \cr
%% $ L3T6: num [1:128, 1:4] \cr
%% $ L4T1: num [1:128, 1:5] \cr
%% $ L4T2: num [1:128, 1:4] \cr
%% $ L4T3: num [1:128, 1:5] \cr
%% $ L4T4: num [1:128, 1:3] \cr
%% $ L4T5: num [1:128, 1:5] \cr
%% $ L4T6: num [1:128, 1:4] \cr
%% $ L5T1: num [1:128, 1:5] \cr
%% $ L5T2: num [1:128, 1:4] \cr
%% $ L5T3: num [1:128, 1:5] \cr
%% $ L5T4: num [1:128, 1:3] \cr
%% $ L5T5: num [1:128, 1:5] \cr
%% $ L5T6: num [1:128, 1:4] \cr
%% $ L6T1: num [1:128, 1:5] \cr
%% $ L6T2: num [1:128, 1:4] \cr
%% $ L6T3: num [1:128, 1:5] \cr
%% $ L6T4: num [1:128, 1:3] \cr
%% $ L6T5: num [1:128, 1:5] \cr
%% $ L6T6: num [1:128, 1:4] \cr
%% $ L7T1: num [1:128, 1:5] \cr
%% $ L7T2: num [1:128, 1:4] \cr
%% $ L7T3: num [1:128, 1:5] \cr
%% $ L7T4: num [1:128, 1:3] \cr
%% $ L7T5: num [1:128, 1:5] \cr
%% $ L7T6: num [1:128, 1:4] \cr
%% $ L8T1: num [1:128, 1:5] \cr
%% $ L8T2: num [1:128, 1:4] \cr
%% $ L8T3: num [1:128, 1:5] \cr
%% $ L8T4: num [1:128, 1:3] \cr
%% $ L8T5: num [1:128, 1:5] \cr
%% $ L8T6: num [1:128, 1:4] \cr
%% $ L9T1: num [1:128, 1:5] \cr
%% $ L9T2: num [1:128, 1:4] \cr
%% $ L9T3: num [1:128, 1:5] \cr
%% $ L9T4: num [1:128, 1:3] \cr
%% $ L9T5: num [1:128, 1:5] \cr
%% $ L9T6: num [1:128, 1:4] \cr
%%}
\details{The specific LOCATIONS of the brain where the signal was measured were [1] Cortex 1: Primary Somatosensory,
Contralateral, [2] Cortex 2: Primary Somatosensory, Ipsilateral, [3] Cortex 3: Secondary Somatosensory, Contralateral, [4] Cortex 4: Secondary Somatosensory, Ipsilateral, [5] Caudate, [6] Thalamus 1: Contralateral, [7] Thalamus 2: Ipsilateral, [8] Cerebellum 1: Contralateral and [9] Cerebellum 2: Ipsilateral. 

The TREATMENTS or stimuli (and number of subjects in each condition) are [1] Awake-Brush (5 subjects), [2] Awake-Heat (4 subjects), [3] Awake-Shock (5 subjects), [4] Low-Brush (3 subjects), [5] Low-Heat (5 subjects), and [6] Low-Shock (4 subjects). Issue the command summary(fmri) for further details.

As an example, \code{fmri$L1T6} (Location 1, Treatment 6) will show the data for the four subjects receiving the Low-Shock treatment at the Cortex 1 location; note that \code{fmri[[6]]} will display the same data.
%%  ~~ If necessary, more details than the __description__ above ~~
}
%%\source{
%%%%  ~~ reference to a publication or URL from which the data were obtained ~~
%%}
\references{\url{http://www.stat.pitt.edu/stoffer/tsa3/}
%%  ~~ possibly secondary sources and usages ~~
}
\keyword{datasets}
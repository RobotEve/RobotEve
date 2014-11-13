
function [chosen_compounds, expected_measurements] = multiSelOpt(compoundFeatureFile, measurements, availMols, K, l, ystdev)
% Gaussian process based optimization for the Eve Robot Scientist
% Ultrasimple, fast version - optimistic heuristic, no hyperparam fitting, simple linear model,
%                             batch composition based on self-belief.
% Computations occur in input space, for very low time and memory complexity 
% in the number of compounds (almost linear, depending on the implementation of '\').
%
% Arguments:
%
% compoundFeatureFile	a file containing the 'compoundLibrary' feature matrix,
% 			where each row is the feature vector of a molecule.
% 			This library file should be in binary or binary-float format for fast loading.
% 			Make sure the matrix is logical if all data elements are binary - this greatly reduces file size and load time.
% 			Logical is not an option, saving as -binary-float saves some disk space 
%			and may reduce disk wait time for very large libs at the cost of very little CPU time.
% 			Do not z-compress - it saves very little space but costs a lot of CPU time.
%
% measurementsFile	a text file containing two columns.
%			the first column are row indices into the library matrix
%			(first library entry has row index 1) 
%			the second column is the real-valued measurement (higher is better)
%
% availableCompoundsFile	a text file containing a vector of integers, 
%				which represent the row indices into the library matrix
%				which are compounds eligible for testing. 
%				Compounds may be omitted e.g. because they are not loaded in
%				the robot, or because they were already tested.
% 
% K	the required number of compounds to be selected for the next experiment
%	(compounds per plate * number of plates per batch)
%
% l	lambda (regularizer)
%	reasonable choice: 0.001
%
% ystdev	level of optimism (number of standard deviations)
%		reasonable choice: 1.0
%
% Output:
%
% chosen_compound	the next experiment to be caried out
%			(an index vector into the library matrix)
%			A vector of length K.
%
% expected_measurements	the predicted measurements for the chosen_compound experiments
%			A vector of length K.

% If uncommented: prevent the rand function to always return the same values.
% rand('state',sum(100*clock));



if (exist(measurements,'file')~=2)
	error('Measurements file not found');
end

if (exist(availMols,'file')~=2)
	error('Available molecules list file not found');
end

if (exist(compoundFeatureFile,'file')~=2)
	error('Compound library feature matrix file not found');
end

disp('loading...');
measurements = load('-ascii',measurements);
availMols = load('-ascii',availMols);
load(compoundFeatureFile,'compoundLibrary'); 

% check input matrices
if (size(measurements,2)~=2)
	error('The measurements matrix should have two columns.');
end

if (size(availMols,2)~=1)
	error('The available compounds data is not a column vector.');
end

if (min(min(availMols),min(measurements(:,1)))<1)
	error('A compound index is smaller than 1.');
end

if (size(compoundLibrary,1)<max(max(availMols),max(measurements(:,1))))
	error('A compound index exceeds the size of the compound library.');
end

chosen_compounds = [];
unknown = setdiff(availMols,measurements(:,1)); % filter out the already tested molecules.

for i=1:K
	
	mX = compoundLibrary(measurements(:,1),:);
	my = measurements(:,2);
	
	ma = (mX'*mX+l*eye(size(mX,2)))\(mX');
	mw = ma*my;
	
	Xunk = compoundLibrary(unknown,:);
	pred_y = Xunk*mw;
	
	pred_devy = sqrt(sum(Xunk*(ma*ma').*Xunk,2))*ystdev;
	opt = pred_y + pred_devy;
	[bestopt,bestj]=max(opt);
	bestdev = pred_devy(bestj);
	chosen_compound = unknown(bestj);
	chosen_compounds = [chosen_compounds; chosen_compound];

	% believe the prediction like gospel
	unknown(bestj) = [];
	measurements = [measurements; chosen_compound pred_y(bestj)];
end
expected_measurements = measurements((1+size(measurements,1)-length(chosen_compounds)):size(measurements,1),2);
return;

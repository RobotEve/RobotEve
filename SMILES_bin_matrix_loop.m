%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% create the binary matrix in Octave
%
% code for use in windows (unix code differences given in comment form)
%
% Kevin Williams 25 Feb 2011
% adapted from Kurt's email dated 14 Nov 2010
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cd "c:/kswfiles/cherrypick_march"
compoundLibrary = load("AllSMILES_fptmatrix_loop.ascii");
% unix: compoundLibrary=load AllSMILES_fptmatrix_loop.ascii
compoundLibrary = compoundLibrary > 0; % create compact logical matrix
save -binary AllSMILES_fptmatrix_loop.bin compoundLibrary;

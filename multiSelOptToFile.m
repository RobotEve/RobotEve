function multiSelOptToFile(compoundFeatureFile, measurements, availMols, K, l, ystdev, selectedCompoundsFile, predictionsFile)
% Gaussian process based optimization for the Eve Robot Scientist.
% Call multiSelOpt and save results to a file.
% 
% Parameters match those of multiSelOpt, plus the names of the files to write output to:
%
% selectedCompoundsFile	a file to store the list of K selected compounds in.
%			(The file contains only row indices into the library matrix.
%			THIS FILE WILL BE RUTHLESSLY OVERWRITTEN
%
% predictionsFile	a file to store both the selected compounds and the predicted measurements in.
%			The output format is CSV.
%			The compound IDs (in the first column) are row indices into the library matrix.
%			THIS FILE WILL BE RUTHLESSLY OVERWRITTEN

printf('Selecting K compounds...\n');
[chosen_compounds, expected_measurements] = multiSelOpt(compoundFeatureFile, measurements, availMols, K, l, ystdev);

%  if (!isequal(size(chosen_compounds), [K, 1]))
%     K
%     size(chosen_compounds)
%     chosen_compounds
%     error('multiSelOptToFile: Expected multiSelOpt to return K compounds');
%  end
%  
%  if (!isequal(size(expected_measurements), [K, 1])) 
%     K
%     size(expected_measurements)
%     expected_measurements
%     error('multiSelOptToFile: Expected multiSelOpt to return K predictions');
%  end

fid = fopen(selectedCompoundsFile,'w');
fprintf(fid,'%g\n',chosen_compounds);
fclose(fid);
printf('Written %g compound indexes for experimentation to %s.\n',length(chosen_compounds),selectedCompoundsFile);

fid = fopen(predictionsFile,'w');
fprintf(fid,'compound_index,prediction\n')
fprintf(fid,'%g,%g\n',[chosen_compounds expected_measurements]);
fclose(fid);
printf('Written the %g selected compounds and their predictions to %s.\n',length(chosen_compounds),predictionsFile);
printf('Done selecting compounds.\n');
end;

/*
 * FingerPrintConvertor.java
 *
 * Created on April 1, 2009, 5:43 PM
 *
 */

package be.kuleuven.cs.robosci;

import be.kuleuven.cs.pubchem.FingerPrintReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.BitSet;
import java.util.List;

/**
 * Convert an open babel fingerprint file to a Matlab/Octave readable binary matrix.
 * If the matrix will be used often, it is recommended to post-process the file with Octave/Matlab
 * to create a file format that Octave/Matlab can load more quickly.
 * Also see the howto file in ~kdg/ActiveLearning/eve/oneatatime/Library 
 *
 * @author kdg
 */
public class FingerPrintConvertor {
    
    /** Creates a new instance of FingerPrintConvertor */
    public FingerPrintConvertor() {
    }
    
    public static void main(String[] args) throws IOException, IllegalArgumentException {
        FingerPrintReader fpReader = new FingerPrintReader();
        if (args.length < 1) {
            System.err.println("Usage:");
            System.err.println("FingerPrintConvertor infile.fpt [outfile]");
            System.err.println("Where infile.fpt is an Open Babel FP2 1024-bit fingerprint file created by:");
            System.err.println("   babel smiles.smi -xfFP2 -xN1024 -xh infile.fpt");
            System.err.println("A fingerprint matrix wil be written to outfile if specified or standard output.");
            throw new IllegalArgumentException("Insuficient number of arguments");
        }
        File inFile = new File(args[0]);
        if (!inFile.exists()) {
            throw new IOException("Input file does not exist.");
        }
        BufferedWriter out = null;
        if (args.length >= 2) {
            File outFile = new File(args[1]);
            out = new BufferedWriter(new FileWriter(outFile));
        } else {
            out = new BufferedWriter(new OutputStreamWriter(System.out));
        }
        List<BitSet> fpts = fpReader.readFingerPrints(inFile,1024);
        System.err.println("Read " + fpts.size() + " fingerprints");
        for (BitSet fpt: fpts) {
            for(int i=0; i<1024;i++) {
                if (fpt.get(i)) {
                    out.write("1 ");
                } else {
                    out.write("0 ");                    
                }
            }
            out.write("\n");
        }
        out.close();
    }
}

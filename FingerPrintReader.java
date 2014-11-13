/*
 * FingerPrintReader.java
 *
 * Created on July 30, 2007, 5:14 PM
 */

package be.kuleuven.cs.pubchem;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;

/**
 *
 * @author kdg
 */
public class FingerPrintReader {
    
    
    /** Creates a new instance of FingerPrintReader */
    public FingerPrintReader() {
    }
    
    /**
     * Read an Openbabel FP2 fingerprint file created with
     * <code>babel mymolecules.sdf -xfFP2 -xn1024 -xh myfingerprints.fpt</code>
     *
     * @pre file != null
     */
    public List<BitSet> readFingerPrints(File file, int bits) throws IOException {
        ArrayList<BitSet> fprints = new ArrayList<BitSet>();
        BufferedReader reader = new BufferedReader(new FileReader(file));
        BitSet fprint = null;
        try {
            String line = reader.readLine();
            while (line != null) {
                if (line.startsWith(">")) {
                    StringBuilder fprintBuilder = new StringBuilder();
                    do {
                        line = reader.readLine();
                        if (! ((line == null) || line.startsWith(">") || line.startsWith("Possible superstructure"))) {
                            fprintBuilder.append(line);
                        }
                    } while (! ((line == null) || line.startsWith(">")));
                    String hex = fprintBuilder.toString().replace(" ", "");
                    fprints.add(hexToBits(hex, bits));
                } else {
                    line = reader.readLine();
                }
            }
            
            if (fprint != null && (line.startsWith("> ") || (line == null))) {
                fprints.add(fprint);
                fprint = new BitSet();
            } else {
                
                if (fprint == null) {
                    fprint = new BitSet();
                }
                
            }
        } finally {
            try {
                reader.close();
            } catch (IOException ioe) {
            }
        }
        return fprints;
    }
    
    
    private byte[] hexToBytes(String s) {
        return hexToBytes(s, 0);
    }
    
    private byte[] hexToBytes(String s, int off) {
        byte[] bs = new byte[off + (1 + s.length()) / 2];
        hexToBytes(s, bs, off);
        return bs;
    }
    
    /**
     * Converts a String of hex characters into an array of bytes.
     *
     * From freenetproject.org freenet.support.HexUtil
     *
     * @param s
     *            A string of hex characters (upper case or lower) of even
     *            length.
     * @param out
     *            A byte array of length at least s.length()/2 + off
     * @param off
     *            The first byte to write of the array
     */
    private void hexToBytes(String s, byte[] out, int off) throws NumberFormatException, IndexOutOfBoundsException {
        int slen = s.length();
        if ((slen % 2) != 0) {
            s = '0' + s;
        }
        
        if (out.length < off + slen / 2) {
            throw new IndexOutOfBoundsException(
                    "Output buffer too small for input ("
                    + out.length
                    + '<'
                    + off
                    + slen / 2
                    + ')');
        }
        
        // Safe to assume the string is even length
        byte b1, b2;
        for (int i = 0; i < slen; i += 2) {
            b1 = (byte) Character.digit(s.charAt(i), 16);
            b2 = (byte) Character.digit(s.charAt(i + 1), 16);
            if ((b1 < 0) || (b2 < 0)) {
                throw new NumberFormatException();
            }
            out[off + i / 2] = (byte) (b1 << 4 | b2);
        }
    }
    
    private BitSet hexToBits(String s, int length) {
        byte[] b = hexToBytes(s);
        BitSet ba = new BitSet(length);
        bytesToBits(b, ba, length);
        return ba;
    }
    
    /**
     * Read a hex string of bits and write it into a bitset
     *
     * From freenetproject.org freenet.support.HexUtil
     *
     * @param s hex string of the stored bits
     * @param ba the bitset to store the bits in
     * @param length the maximum number of bits to store
     */
    protected void hexToBits(String s, BitSet ba, int length) {
        byte[] b = hexToBytes(s);
        bytesToBits(b, ba, length);
    }
    
    /**
     * Read bits from a byte array into a bitset
     *
     * From freenetproject.org freenet.support.HexUtil
     * but with bugfix.
     *
     * @param b the byte[] to read from
     * @param ba the bitset to write to
     */
    private void bytesToBits(byte[] b, BitSet ba, int maxSize) {
        int x = 0;
        for(int i=0;i<b.length;i++) {
            for(int j=0;j<8;j++) {
                if(x > maxSize) break;
                int mask = 128 >> j;  // fixed a bug
                boolean value = (mask & b[i]) != 0;
                ba.set(x, value);
                x++;
            }
        }
    }
}

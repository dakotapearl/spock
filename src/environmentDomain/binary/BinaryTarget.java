package environmentDomain.binary;

import dataDomain.Datum;

public interface BinaryTarget {
	
	public void acceptBit(int channel, boolean bit);
	
}

package environmentDomain.binary;

import dataDomain.DataDomain;
import tools.errorChecking.Assert;
import networkDomain.NetworkSignal;
import environmentDomain.Action;

public class BitAction extends Action {

	BinaryTarget target;
	int channel;
	
	public BitAction(BinaryTarget target, int channel) {
		this.target = target;
		this.channel = channel;
	}
	
	@Override
	public void performAction(NetworkSignal signal) {
		Assert.AssertTrue("Signal has binary value", signal.getData().getDatum().getType() == DataDomain.DATUM_TYPE_BOOLEAN);
		
		target.acceptBit(channel, (Boolean) signal.getData().getDatum().getValue());
	}

	@Override
	public void start() {
		
	}

}

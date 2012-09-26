package networkDomain.extensions.implementations.Test;

import networkDomain.NetworkSignal;
import networkDomain.NetworkTargetable;
import networkDomain.extensions.TargetSelection;

public class ts extends TargetSelection {

	@Override
	public NetworkTargetable selectTarget(NetworkSignal signal) {
		//tools.Log.write("Test Extension: TargetingSelection received selectTarget command");
		
		return parent.storageProcess.getAction(0);
	}

	@Override
	public TargetSelection replicate() {
		// TODO Auto-generated method stub
		return null;
	}

}

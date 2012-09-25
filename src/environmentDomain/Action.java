package environmentDomain;

import networkDomain.NetworkSignal;
import networkDomain.NetworkTargetable;
import networkDomain.NetworkTransmitter;
import tools.errorChecking.Log;

public abstract class Action implements NetworkTargetable {

	public Action() {
		Log.write("Action initialised: " + this.getClass().getSimpleName());
	}
	
	@Override
	public void acceptSignal(NetworkSignal signal, NetworkTransmitter sender) {
		performAction(signal);
	}
	
	public abstract void performAction(NetworkSignal signal);
	public abstract void start();
	
}

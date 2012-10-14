package environmentDomain;

import networkDomain.NetworkSignal;
import networkDomain.NetworkTargetable;
import networkDomain.NetworkTransmitter;
import tools.errorChecking.Log;

/**
 * @author Loren Chorley
 */
public abstract class Action implements NetworkTargetable {

	private int id;
	
	public Action(int id) {
		Log.write("Action initialised: " + this.getClass().getSimpleName());
		this.id = id;
	}
	
	@Override
	public void acceptSignal(NetworkSignal signal, NetworkTransmitter sender) {
		Log.writeForMechanisms("Action: Accepted signal with datum: " + signal.getData().getDatum().getValue().toString());
		
		performAction(signal);
	}
	
	public int getID() {
		return id;
	}
	
	public abstract void performAction(NetworkSignal signal);
	public abstract void start();
	
}

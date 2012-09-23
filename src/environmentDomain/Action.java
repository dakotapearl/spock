package environmentDomain;

import tools.Log;
import networkDomain.NetworkSignal;

public abstract class Action {

	public Action() {
		Log.write("Action initialised: " + this.getClass().getSimpleName());
	}
	
	public abstract void performAction(NetworkSignal signal);
	
}

package environmentDomain;

import networkDomain.NetworkSignal;
import tools.errorChecking.Log;

public abstract class Action {

	public Action() {
		Log.write("Action initialised: " + this.getClass().getSimpleName());
	}
	
	public abstract void performAction(NetworkSignal signal);
	
}

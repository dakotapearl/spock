package environmentsDomain;

import tools.Log;
import networkDomain.NetworkComponent;
import networkDomain.NetworkSignal;

public abstract class Action extends NetworkComponent {

	public Action() {
		Log.write("Action initialised: " + this.getClass().getSimpleName());
	}
	
	public abstract void performAction(NetworkSignal signal);
	
}

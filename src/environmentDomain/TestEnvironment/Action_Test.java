package environmentDomain.TestEnvironment;

import networkDomain.NetworkSignal;
import tools.errorChecking.Log;
import environmentDomain.Action;

/**
 * @author Loren Chorley
 */
public class Action_Test extends Action {

	@Override
	public void performAction(NetworkSignal signal) {
		Log.write("ACTION PERFORMED: Test, with datum: " + signal.getData().getDatum().getValue());
	}

	@Override
	public void start() {
		
	}

}

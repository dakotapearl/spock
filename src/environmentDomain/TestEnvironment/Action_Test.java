package environmentDomain.TestEnvironment;

import networkDomain.NetworkSignal;
import tools.errorChecking.Log;
import environmentDomain.Action;

/**
 * @author Loren Chorley
 */
public class Action_Test extends Action {

	public Action_Test(int id) {
		super(id);
	}

	@Override
	public void performAction(NetworkSignal signal) {
		Log.write("ACTION PERFORMED: Test, with datum: " + signal.getData().getDatum().getValue());
	}

	@Override
	public void start() {
		
	}

	@Override
	public int getID() {
		// TODO Auto-generated method stub
		return 0;
	}

}

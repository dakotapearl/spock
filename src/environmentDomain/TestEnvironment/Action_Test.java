package environmentDomain.TestEnvironment;

import networkDomain.NetworkSignal;
import dataDomain.DataCell;
import tools.Log;
import environmentDomain.Action;

public class Action_Test extends Action {

	@Override
	public void performAction(NetworkSignal signal) {
		Log.write("ACTION PERFORMED: Test, with datum: " + signal.getData(null).getDatum().getValue());
		
	}

}

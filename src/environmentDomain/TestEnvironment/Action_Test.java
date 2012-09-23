package environmentsDomain.testing;

import dataDomain.DataCell;
import tools.Log;
import environmentsDomain.Action;

public class Action_Test extends Action {

	@Override
	public void performAction(DataCell dataCell) {
		Log.write("ACTION PERFORMED: Test, with datum: " + dataCell.getDatum().getValue());
	}

}

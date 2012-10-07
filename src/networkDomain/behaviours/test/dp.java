package networkDomain.behaviours.test;

import dataDomain.DataCell;
import networkDomain.extensions.DataProcessing;

/**
 * @author Loren Chorley
 */
public class dp extends DataProcessing {

	@Override
	public DataCell processData(DataCell dataCell) {
		//tools.Log.write("Test Extension: DataProcessing received processData command");
		return new DataCell(1, dataCell.getDatum());
	}

	@Override
	public DataProcessing replicate() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void run() {
		// TODO Auto-generated method stub
		
	}

}

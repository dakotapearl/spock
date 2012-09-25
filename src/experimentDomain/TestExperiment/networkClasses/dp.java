package experimentDomain.TestExperiment.networkClasses;

import dataDomain.DataCell;
import networkDomain.extensions.DataProcessing;

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

}

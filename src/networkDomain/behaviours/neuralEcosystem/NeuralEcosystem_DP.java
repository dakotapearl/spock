package networkDomain.behaviours.neuralEcosystem;

import dataDomain.DataCell;
import networkDomain.extensions.DataProcessing;

/**
 * @author Loren Chorley
 */
public class NeuralEcosystem_DP extends DataProcessing {

	@Override
	public DataCell processData(DataCell dataCell) {
		
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

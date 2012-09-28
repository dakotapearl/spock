package networkDomain.extensions.implementations.Test;

import dataDomain.DataCell;
import networkDomain.NetworkSignal;
import networkDomain.extensions.TransmissionContent;
import networkDomain.signals.ImpulseSignal;

/**
 * @author Loren Chorley
 */
public class tc extends TransmissionContent {

	@Override
	public NetworkSignal nextSignalToFire() {
		//tools.Log.write("Test Extension: TransmissionContent received nextDataCellToFire command");
		while (true) {
			try {
				DataCell dc = parent.storageProcess.retrieveDataCell();
				ImpulseSignal s = new ImpulseSignal();
				s.setData(dc);
				return s; //return values
			} catch (InterruptedException e) {}
		}
	}

	@Override
	public boolean signalsRemain() {
		//tools.Log.write("Test Extension: TransmissionContent received dataRemains command");
		return parent.storageProcess.hasData();
	}

	@Override
	public TransmissionContent replicate() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void run() {
		// TODO Auto-generated method stub
		
	}

}

package environmentDomain.binary.AND;

import networkDomain.signals.ImpulseSignal;
import tools.concurrency.Port;
import tools.errorChecking.Assert;
import tools.errorChecking.Log;
import dataDomain.DataCell;
import dataDomain.DataDomain;
import dataDomain.Datum;
import environmentDomain.EnvironmentDomain;
import environmentDomain.binary.BinaryTarget;
import environmentDomain.binary.BitAction;
import environmentDomain.binary.BitPerception;
import environmentDomain.types.SensoryMotorSystem;

/**
 * @author Loren Chorley
 */
public class BinaryANDEnvironment extends SensoryMotorSystem implements BinaryTarget {
	
	boolean bit1 = false;
	boolean bit2 = false;
	boolean result = false;
	Port<Boolean> actionsPerformed;
	
	public BinaryANDEnvironment(EnvironmentDomain environmentDomain) {
		super(environmentDomain);
		Assert.AssertTrue("EnvironmentDomain correctly passed to BinaryANDEnvironment", environmentDomain != null);
		
		actions.add(new BitAction(this, 1));
		actions.add(new BitAction(this, 2));
		perceptions.add(new BitPerception(environmentDomain));
		actionsPerformed = new Port<Boolean>();
	}

	@Override
	public void startEnvironment() {
		boolean bitReveived;
		while (true) {
			refresh();
			
			try {
				actionsPerformed.receive();
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		
	}

	@Override
	public void acceptBit(int channel, boolean bit) {
		Log.write("BinaryTarget: received " + (bit ? "true" : "false") + " via channel " + String.valueOf(channel));
		
		if (channel == 1) {
			bit1 = bit;
		} else if (channel == 2) {
			bit2 = bit;
		} else
			Assert.CriticalAssertTrue("Should not reach here", false);
		
		refresh();
	}

	public void refresh() {
		result = bit1 && bit2;
		ImpulseSignal s = new ImpulseSignal();
		Datum d = environmentDomain.dataDomain.getDatum(DataDomain.DATUM_TYPE_BOOLEAN, result);
		DataCell dc = new DataCell(1, d); 
		s.setData(dc);
		perceptions.get(0).sendSignalToNetwork(s);
	}
	
}

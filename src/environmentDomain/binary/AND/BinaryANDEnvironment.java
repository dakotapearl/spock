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
	Port<actionSet> actionsPerformed;
	
	private class actionSet {
		public actionSet(boolean argb, int argc) {b = argb; c = argc;}
		boolean b;
		int c;
	}
	
	public BinaryANDEnvironment(EnvironmentDomain environmentDomain) {
		super(environmentDomain);
		Assert.AssertTrue("EnvironmentDomain correctly passed to BinaryANDEnvironment", environmentDomain != null);
		
		actions.add(new BitAction(environmentDomain.networkDomain.getUniqueNodeID(), this, 1));
		actions.add(new BitAction(environmentDomain.networkDomain.getUniqueNodeID(), this, 2));
		perceptions.add(new BitPerception(environmentDomain));
		actionsPerformed = new Port<actionSet>();
	}

	@Override
	public void startEnvironment() {
		actionSet bitReveived = null;
		
		// Send initial configuration
		refresh();
		
		while (true) {
			
			try {
				bitReveived = actionsPerformed.receive();
			} catch (InterruptedException e) {}
			
			if (bitReveived != null) {
				
				if (bitReveived.c == 1) {
					bit1 = bitReveived.b;
				} else if (bitReveived.c == 2) {
					bit2 = bitReveived.b;
				} else
					Assert.CriticalAssertTrue("Should not reach here", false);
				
				refresh();
				
			}
			
		}
		
	}

	@Override
	public void acceptBit(int channel, boolean bit) {
		Log.write("BinaryTarget: received " + (bit ? "true" : "false") + " via channel " + String.valueOf(channel));
		
		actionsPerformed.send(new actionSet(bit,channel));
		
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



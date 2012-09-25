package environmentDomain.binary.AND;

import networkDomain.signals.ImpulseSignal;
import tools.errorChecking.Assert;
import dataDomain.DataCell;
import dataDomain.DataDomain;
import environmentDomain.EnvironmentDomain;
import environmentDomain.binary.BinaryTarget;
import environmentDomain.binary.BitAction;
import environmentDomain.binary.BitPerception;
import environmentDomain.types.SensoryMotorSystem;

public class BinaryANDEnvironment extends SensoryMotorSystem implements BinaryTarget {
	
	boolean bit1 = false;
	boolean bit2 = false;
	boolean result = false;
	
	public BinaryANDEnvironment(EnvironmentDomain environmentDomain) {
		super(environmentDomain);
		actions.add(new BitAction(this, 1));
		actions.add(new BitAction(this, 2));
		perceptions.add(new BitPerception(environmentDomain));
	}

	@Override
	public void startEnvironment() {
		refresh();
	}

	@Override
	public void acceptBit(int channel, boolean bit) {
		if (channel == 1) {
			bit1 = bit;
		} else if (channel == 2) {
			bit2 = bit;
		} else
			Assert.AssertTrue("Should not reach here", false);
	}

	public void refresh() {
		result = bit1 && bit2;
		ImpulseSignal s = new ImpulseSignal();
		s.setData(new DataCell(1, environmentDomain.dataDomain.getDatum(DataDomain.DATUM_TYPE_BOOLEAN, result)));
		perceptions.get(0).sendSignalToNetwork(s);
	}
	
}

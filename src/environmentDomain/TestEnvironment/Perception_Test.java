package environmentDomain.TestEnvironment;

import tools.errorChecking.Assert;
import dataDomain.DataCell;
import dataDomain.DataDomain;
import dataDomain.Datum;
import environmentDomain.EnvironmentDomain;
import environmentDomain.Perception;
import networkDomain.NetworkSignal;
import networkDomain.signals.ImpulseSignal;

public class Perception_Test extends Perception {

	public Perception_Test(EnvironmentDomain environmentDomain) {
		super(environmentDomain);
		Assert.AssertTrue("environmentDomain set correctly", environmentDomain != null);
	}

	@Override
	public void start() {
		Datum d;
		DataCell dc;
		NetworkSignal s;
		
		//while (true) {

			d = environmentDomain.dataDomain.getDatum(DataDomain.DATUM_TYPE_BOOLEAN, true);
			dc = new DataCell(1, d);
			s = new ImpulseSignal();
			s.setData(dc);
			sendSignalToNetwork(s);
			
			/*try {
				w
			} catch (InterruptedException e) {}*/
			
			d = environmentDomain.dataDomain.getDatum(DataDomain.DATUM_TYPE_BOOLEAN, false);
			dc = new DataCell(1, d);
			s = new ImpulseSignal();
			s.setData(dc);
			sendSignalToNetwork(s);
		
		
		//}
		
	}


}

package environmentsDomain.testing;

import dataDomain.DataCell;
import dataDomain.DataDomain;
import dataDomain.Datum;
import environmentsDomain.Perception;
import networkDomain.NetworkNode;

public class Perception_Test extends Perception {

	public Perception_Test(DataDomain dataDomain) {
		super(dataDomain);
	}
	
	public synchronized void go() {
		
		Datum d = dataDomain.getDatum(DataDomain.DATUM_TYPE_BOOLEAN, true);
		DataCell dc = new DataCell(1, d);
		for (NetworkNode s : sensors) {
			s.acceptData(dc, null);
		}
		try {
			this.wait(2000);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		d = dataDomain.getDatum(DataDomain.DATUM_TYPE_BOOLEAN, false);
		dc = new DataCell(1, d);
		for (NetworkNode s : sensors) {
			s.acceptData(dc, null);
		}
	}

	@Override
	protected void accessibilityFunction() {
		tools.Log.write("Perception made accessible!");
		go();
	}

	@Override
	protected void inaccessibilityFunction() {
		
	}

	@Override
	public boolean isInitiallyAccessible() {
		return true;
	}
	
}
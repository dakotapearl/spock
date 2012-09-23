package experimentsDomain;

import networkDomain.NetworkConfiguration;
import networkDomain.NetworkDomain;
import environmentsDomain.EnvironmentsDomain;
import experimentsDomain.test.Config_Test;
import experimentsDomain.textual.Textual_Config1;
import tools.Assert;
import tools.Log;
import mechanismsDomain.Domain;

public class ExperimentsDomain extends Domain implements ExperimentsToEnvironmentsClientInterface, ExperimentsToNetworkClientInterface {
	
	private ExperimentsToNetworkServerInterface networkEE;
	private ExperimentsToEnvironmentsServerInterface environmentEE;
	
	public static final int XID_TEST = 0;
	public static final int XID_TEXTUAL_ONE = 1;
	
	private Integer xid = null;
	
	private Experiment experiment;
	
	public void initialise() {
		Assert.CriticalAssertTrue("XID is set", xid != null);
		Log.write("Experiments domain initialised");
		
		experiment = new Experiment();
		
		Log.write("Experiments Domain (1): Making config");
		NetworkConfiguration config = null;
		switch (xid) {
			case 0:
				config = new Config_Test();
				break;
			case 1:
				config = new Textual_Config1();
		}
		
		Log.write("Experiments Domain (2): Building experiment from config");
		config.buildNetwork(this);
		
		Log.write("Experiments Domain (3): Readying experiment");
		experiment.readyExperiment();
		
	}
	
	public void setNetworkEE(ExperimentsToNetworkServerInterface networkEE) {
		this.networkEE = networkEE;
	}
	
	public NetworkDomain getNetworkDomain() {
		return (NetworkDomain) networkEE;
	}
	
	public void setEnvironmentEE(ExperimentsToEnvironmentsServerInterface environmentEE) {
		this.environmentEE = environmentEE;
	}

	public EnvironmentsDomain getEnvironmentsDomain() {
		return (EnvironmentsDomain) environmentEE;
	}
	
	public void setExperiment(int xid) {
		this.xid = xid;
	}
	
	public Experiment getExperiment() {
		return experiment;
	}
	
}

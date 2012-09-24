package experimentDomain;

import networkDomain.NetworkDomain;
import environmentDomain.EnvironmentDomain;
import tools.errorChecking.Log;

public class ExperimentDomain {

	public EnvironmentDomain environmentDomain;
	public NetworkDomain networkDomain;
	
	public void initialise() {
		// check that all relevant domains are linked
		
		Log.write("Experiment domain initialised");
	}
	
	public void setEnvironmentDomain(EnvironmentDomain environmentDomain) {
		this.environmentDomain = environmentDomain;
	}
	
	public void setNetworkDomain(NetworkDomain networkDomain) {
		this.networkDomain = networkDomain;
	}
	
}

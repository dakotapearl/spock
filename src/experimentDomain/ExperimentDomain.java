package experimentDomain;

import networkDomain.NetworkDomain;
import environmentDomain.EnvironmentDomain;
import tools.errorChecking.Assert;
import tools.errorChecking.Log;

public class ExperimentDomain {

	public EnvironmentDomain environmentDomain;
	public NetworkDomain networkDomain;
	
	public void initialise() {
		Assert.AssertTrue("ExperimentDomain: EnvironmentDomain is connected", environmentDomain != null);
		Assert.AssertTrue("ExperimentDomain: NetworkDomain is connected", networkDomain != null);
		
		Log.write("Experiment domain initialised");
	}
	
	public void setEnvironmentDomain(EnvironmentDomain environmentDomain) {
		this.environmentDomain = environmentDomain;
	}
	
	public void setNetworkDomain(NetworkDomain networkDomain) {
		this.networkDomain = networkDomain;
	}
	
}

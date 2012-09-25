package networkDomain;

import java.util.ArrayList;

import tools.errorChecking.Log;
import dataDomain.DataDomain;
import environmentDomain.EnvironmentDomain;
import experimentDomain.ExperimentDomain;

public class NetworkDomain {
	
	public EnvironmentDomain environmentDomain;
	public ExperimentDomain experimentDomain;
	public DataDomain dataDomain;
	ArrayList<NetworkNode> network;
	
	public void initialise() {
		// check that all relevant domains are linked
		
		Log.write("Network domain initialised");
	}
	
	public void setEnvironmentDomain(EnvironmentDomain environmentDomain) {
		this.environmentDomain = environmentDomain;
	}
	
	public void setExperimentDomain(ExperimentDomain experimentDomain) {
		this.experimentDomain = experimentDomain;
	}
	
	public void setDataDomain(DataDomain dataDomain) {
		this.dataDomain = dataDomain;
	}

	public ArrayList<NetworkNode> getNetwork() {
		return network;
	}
	
}

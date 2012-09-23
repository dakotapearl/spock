package interfaceDomain;

import dataDomain.DataDomain;
import networkDomain.NetworkDomain;
import environmentDomain.EnvironmentDomain;
import experimentDomain.ExperimentDomain;
import tools.Log;

public class InterfaceDomain {

	EnvironmentDomain environmentDomain;
	ExperimentDomain experimentDomain;
	NetworkDomain networkDomain;
	DataDomain dataDomain;
	
	public void initialise() {
		
		// Applet perhaps, that can display relevant details as well as pick, control, start, restart and stop experiments
		// turn off and on various logging comments
		
		Log.write("Interface domain initialised");
	}
	
	public void setEnvironmentDomain(EnvironmentDomain environmentDomain) {
		this.environmentDomain = environmentDomain;
	}
	
	public void setExperimentDomain(ExperimentDomain experimentDomain) {
		this.experimentDomain = experimentDomain;
	}
	
	public void setNetworkDomain(NetworkDomain networkDomain) {
		this.networkDomain = networkDomain;
	}
	
	public void setDataDomain(DataDomain dataDomain) {
		this.dataDomain = dataDomain;
	}
	
}

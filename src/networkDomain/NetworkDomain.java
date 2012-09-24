package networkDomain;

import java.util.ArrayList;

import tools.errorChecking.Log;
import networkDomain.exceptions.InvalidNetworkException;
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
	
	public void initialiseNetwork(NetworkInitialiser initialiser) throws InvalidNetworkException {
		network = initialiser.initialiseNetwork();
		if (network == null)
			throw new InvalidNetworkException();
		if (network.size() == 0)
			throw new InvalidNetworkException();
	}
	
}

package experimentDomain;

import configurationDomain.ConfigurationDomain;
import application.Domain;
import application.DomainContainer;
import interfaceDomain.InterfaceDomain;
import networkDomain.NetworkDomain;
import environmentDomain.EnvironmentDomain;
import tools.errorChecking.Log;

/**
 * @author Loren Chorley
 */
public class ExperimentDomain extends Domain {

	public ExperimentDomain(DomainContainer container) {
		super(container);
		environmentDomain = container.environmentDomain;
		networkDomain = container.networkDomain;
		interfaceDomain = container.interfaceDomain;
		configurationDomain = container.configurationDomain;
	}

	public EnvironmentDomain environmentDomain;
	public NetworkDomain networkDomain;
	public InterfaceDomain interfaceDomain;
	public ConfigurationDomain configurationDomain;
	
	public void initialise() {
		Log.write("Experiment domain initialised");
	}
	
}

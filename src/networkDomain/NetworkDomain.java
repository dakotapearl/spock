package networkDomain;

import configurationDomain.ConfigurationDomain;
import application.Domain;
import application.DomainContainer;
import interfaceDomain.InterfaceDomain;
import metricDomain.MetricDomain;
import tools.errorChecking.Log;
import dataDomain.DataDomain;
import environmentDomain.EnvironmentDomain;
import experimentDomain.ExperimentDomain;

/**
 * @author Loren Chorley
 */
public class NetworkDomain extends Domain {
	
	public NetworkDomain(DomainContainer container) {
		super(container);
		environmentDomain = container.environmentDomain;
		experimentDomain = container.experimentDomain;
		interfaceDomain = container.interfaceDomain;
		dataDomain = container.dataDomain;
		metricDomain = container.metricDomain;
		configurationDomain = container.configurationDomain;
	}

	public EnvironmentDomain environmentDomain;
	public ExperimentDomain experimentDomain;
	public DataDomain dataDomain;
	public MetricDomain metricDomain;
	public InterfaceDomain interfaceDomain;
	public ConfigurationDomain configurationDomain;
	
	Network network;
	int nodeUID = 0;
	
	public void setNetwork(Network network) {
		this.network = network;
	}
	
	public Network getNetwork() {
		return network;
	}
	
	public synchronized int getUniqueNodeID() {
		return nodeUID++;
	}

	@Override
	public void initialiseIndependent() {
		environmentDomain = container.environmentDomain;
		experimentDomain = container.experimentDomain;
		interfaceDomain = container.interfaceDomain;
		dataDomain = container.dataDomain;
		metricDomain = container.metricDomain;
		configurationDomain = container.configurationDomain;
		
		Log.write("Network domain initialised (independent)");
	}

	@Override
	public void initialiseInterconnected() {
		configurationDomain.loadConfigWithStandardHandling("config/network.xml", "network");
		
		Log.write("Network domain initialised (interconnected)");
	}
	
}

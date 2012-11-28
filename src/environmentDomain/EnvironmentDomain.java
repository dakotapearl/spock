package environmentDomain;

import configurationDomain.ConfigurationDomain;
import application.Domain;
import application.DomainContainer;
import interfaceDomain.InterfaceDomain;
import metricDomain.MetricDomain;
import networkDomain.NetworkDomain;
import dataDomain.DataDomain;
import experimentDomain.ExperimentDomain;
import tools.errorChecking.Log;

/**
 * @author Loren Chorley
 */
public class EnvironmentDomain extends Domain {
	
	public EnvironmentDomain(DomainContainer container) {
		super(container);
	}

	public ExperimentDomain experimentDomain;
	public NetworkDomain networkDomain;
	public DataDomain dataDomain;
	public MetricDomain metricDomain;
	public InterfaceDomain interfaceDomain;
	public ConfigurationDomain configurationDomain;
	
	@Override
	public void initialiseIndependent() {
		experimentDomain = container.experimentDomain;
		networkDomain = container.networkDomain;
		dataDomain = container.dataDomain;
		metricDomain = container.metricDomain;
		interfaceDomain = container.interfaceDomain;
		configurationDomain = container.configurationDomain;
		
		Log.write("Environment domain initialised (indepentent)");
	}

	@Override
	public void initialiseInterconnected() {
		Log.write("Environment domain initialised (interconnected)");
	}
	
}

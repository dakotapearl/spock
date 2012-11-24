package application;

import configurationDomain.ConfigurationDomain;
import interfaceDomain.InterfaceDomain;
import metricDomain.MetricDomain;
import networkDomain.NetworkDomain;
import dataDomain.DataDomain;
import environmentDomain.EnvironmentDomain;
import experimentDomain.ExperimentDomain;

public class DomainContainer {
	
	public EnvironmentDomain environmentDomain;
	public ExperimentDomain experimentDomain;
	public NetworkDomain networkDomain;
	public DataDomain dataDomain;
	public MetricDomain metricDomain;
	public ConfigurationDomain configurationDomain;
	public InterfaceDomain interfaceDomain;
	
}

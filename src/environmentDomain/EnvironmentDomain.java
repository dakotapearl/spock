package environmentDomain;

import interfaceDomain.InterfaceDomain;
import metricDomain.MetricDomain;
import networkDomain.NetworkDomain;
import dataDomain.DataDomain;
import experimentDomain.ExperimentDomain;
import tools.errorChecking.Assert;
import tools.errorChecking.Log;

/**
 * @author Loren Chorley
 */
public class EnvironmentDomain {
	
	public ExperimentDomain experimentDomain;
	public NetworkDomain networkDomain;
	public DataDomain dataDomain;
	public MetricDomain metricDomain;
	public InterfaceDomain interfaceDomain;
	
	public void initialise() {
		Assert.AssertTrue("EnvironmentDomain: ExperimentDomain is connected", experimentDomain != null);
		Assert.AssertTrue("EnvironmentDomain: NetworkDomain is connected", networkDomain != null);
		Assert.AssertTrue("EnvironmentDomain: DataDomain is connected", dataDomain != null);
		Assert.AssertTrue("EnvironmentDomain: MetricDomain is connected", metricDomain != null);
		
		Log.write("Environment domain initialised");
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
	
	public void setMetricDomain(MetricDomain metricDomain) {
		this.metricDomain = metricDomain;
	}
	
	public void setInterfaceDomain(InterfaceDomain interfaceDomain) {
		this.interfaceDomain = interfaceDomain;
	}
	
}

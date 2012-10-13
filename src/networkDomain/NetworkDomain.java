package networkDomain;

import metricDomain.MetricDomain;
import tools.errorChecking.Assert;
import tools.errorChecking.Log;
import dataDomain.DataDomain;
import environmentDomain.EnvironmentDomain;
import experimentDomain.ExperimentDomain;

/**
 * @author Loren Chorley
 */
public class NetworkDomain {
	
	public EnvironmentDomain environmentDomain;
	public ExperimentDomain experimentDomain;
	public DataDomain dataDomain;
	public MetricDomain metricDomain;
	Network network;
	
	public void initialise() {
		Assert.AssertTrue("NetworkDomain: EnvironmentDomain is connected", environmentDomain != null);
		Assert.AssertTrue("NetworkDomain: ExperimentDomain is connected", experimentDomain != null);
		Assert.AssertTrue("NetworkDomain: DataDomain is connected", dataDomain != null);
		Assert.AssertTrue("NetworkDomain: MetricDomain is connected", metricDomain != null);
		
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

	public void setMetricDomain(MetricDomain metricDomain) {
		this.metricDomain = metricDomain;
	}
	
	public void setNetwork(Network network) {
		this.network = network;
	}
	
	public Network getNetwork() {
		return network;
	}
	
}

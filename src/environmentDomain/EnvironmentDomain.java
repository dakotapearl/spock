package environmentDomain;

import networkDomain.NetworkDomain;
import dataDomain.DataDomain;
import experimentDomain.ExperimentDomain;
import goalDomain.GoalDomain;
import tools.errorChecking.Assert;
import tools.errorChecking.Log;

/**
 * @author Loren Chorley
 */
public class EnvironmentDomain {
	
	public ExperimentDomain experimentDomain;
	public NetworkDomain networkDomain;
	public DataDomain dataDomain;
	public GoalDomain goalDomain;
	
	public void initialise() {
		Assert.AssertTrue("EnvironmentDomain: ExperimentDomain is connected", experimentDomain != null);
		Assert.AssertTrue("EnvironmentDomain: NetworkDomain is connected", networkDomain != null);
		Assert.AssertTrue("EnvironmentDomain: DataDomain is connected", dataDomain != null);
		Assert.AssertTrue("EnvironmentDomain: GoalDomain is connected", goalDomain != null);
		
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
	
	public void setGoalDomain(GoalDomain goalDomain) {
		this.goalDomain = goalDomain;
	}
	
	
}

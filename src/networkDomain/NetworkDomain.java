package networkDomain;

import java.util.ArrayList;

import tools.errorChecking.Assert;
import tools.errorChecking.Log;
import dataDomain.DataDomain;
import environmentDomain.EnvironmentDomain;
import experimentDomain.ExperimentDomain;
import goalDomain.GoalDomain;

public class NetworkDomain {
	
	public EnvironmentDomain environmentDomain;
	public ExperimentDomain experimentDomain;
	public DataDomain dataDomain;
	public GoalDomain goalDomain;
	ArrayList<NetworkNode> network;
	
	public void initialise() {
		Assert.AssertTrue("NetworkDomain: EnvironmentDomain is connected", environmentDomain != null);
		Assert.AssertTrue("NetworkDomain: ExperimentDomain is connected", experimentDomain != null);
		Assert.AssertTrue("NetworkDomain: DataDomain is connected", dataDomain != null);
		Assert.AssertTrue("NetworkDomain: GoalDomain is connected", goalDomain != null);
		
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

	public void setGoalDomain(GoalDomain goalDomain) {
		this.goalDomain = goalDomain;
	}
	
	public ArrayList<NetworkNode> getNetwork() {
		return network;
	}
	
}

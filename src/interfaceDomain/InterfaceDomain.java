package interfaceDomain;

import dataDomain.DataDomain;
import networkDomain.NetworkDomain;
import environmentDomain.EnvironmentDomain;
import experimentDomain.ExperimentDomain;
import goalDomain.GoalDomain;
import tools.errorChecking.Assert;
import tools.errorChecking.Log;

/**
 * @author Loren Chorley
 */
public class InterfaceDomain {

	EnvironmentDomain environmentDomain;
	ExperimentDomain experimentDomain;
	NetworkDomain networkDomain;
	DataDomain dataDomain;
	GoalDomain goalDomain;
	
	public void initialise() {
		Assert.AssertTrue("InterfaceDomain: EnvironmentDomain is connected", environmentDomain != null);
		Assert.AssertTrue("InterfaceDomain: ExperimentDomain is connected", experimentDomain != null);
		Assert.AssertTrue("InterfaceDomain: DataDomain is connected", dataDomain != null);
		Assert.AssertTrue("InterfaceDomain: GoalDomain is connected", goalDomain != null);
		Assert.AssertTrue("InterfaceDomain: NetworkDomain is connected", networkDomain != null);
		
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
	
	public void setGoalDomain(GoalDomain goalDomain) {
		this.goalDomain = goalDomain;
	}
	
}

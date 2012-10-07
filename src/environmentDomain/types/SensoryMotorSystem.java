package environmentDomain.types;

import environmentDomain.Action;
import environmentDomain.Environment;
import environmentDomain.EnvironmentDomain;
import environmentDomain.Perception;
import java.util.ArrayList;
import metricDomain.Metric;
import tools.errorChecking.Assert;
import tools.errorChecking.Log;

/**
 * @author Loren Chorley
 */
public abstract class SensoryMotorSystem extends Environment {
	
	protected ArrayList<Action> actions;
	protected ArrayList<Perception> perceptions;
	protected ArrayList<Metric> metrics;
	
	public SensoryMotorSystem(EnvironmentDomain environmentDomain) {
		super(environmentDomain);
		Assert.AssertTrue("EnvironmentDomain correctly passed to SensoryMotorSystem", environmentDomain != null);
		
		actions = new ArrayList<Action>();
		perceptions = new ArrayList<Perception>();
		metrics = new ArrayList<Metric>();
	}
	
	public ArrayList<Action> getActions() {
		return actions;
	}
	
	public ArrayList<Perception> getPerceptions() {
		return perceptions;
	}
	
	public ArrayList<Metric> getGoals() {
		return metrics;
	}
	
	public void run() {
		Assert.AssertTrue("SensoryMotorSystem started with environmentDomain not null", environmentDomain != null);
		
		Log.writeForThreadCreation("Environment");
		Assert.CriticalAssertTrue("Environment node thread started", this.isAlive());
		
		for (Metric m : metrics)
			m.start();
		for (Action a : actions)
			a.start();
		for (Perception p : perceptions)
			p.start();
		startEnvironment();
	}
	
	public abstract void startEnvironment();
	
}

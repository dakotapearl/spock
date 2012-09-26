package environmentDomain.types;

import environmentDomain.Action;
import environmentDomain.Environment;
import environmentDomain.EnvironmentDomain;
import environmentDomain.Perception;
import goalDomain.Goal;

import java.util.ArrayList;

import tools.errorChecking.Assert;

public abstract class SensoryMotorSystem extends Environment {
	
	protected ArrayList<Action> actions;
	protected ArrayList<Perception> perceptions;
	protected ArrayList<Goal> goals;
	
	public SensoryMotorSystem(EnvironmentDomain environmentDomain) {
		super(environmentDomain);
		Assert.AssertTrue("EnvironmentDomain correctly passed to SensoryMotorSystem", environmentDomain != null);
		
		actions = new ArrayList<Action>();
		perceptions = new ArrayList<Perception>();
		goals = new ArrayList<Goal>();
	}
	
	public ArrayList<Action> getActions() {
		return actions;
	}
	
	public ArrayList<Perception> getPerceptions() {
		return perceptions;
	}
	
	public ArrayList<Goal> getGoals() {
		return goals;
	}
	
	public void start() {
		Assert.AssertTrue("SensoryMotorSystem started with environmentDomain not null", environmentDomain != null);
		
		for (Goal g : goals)
			g.start();
		for (Action a : actions)
			a.start();
		for (Perception p : perceptions)
			p.start();
		startEnvironment();
	}
	
	public abstract void startEnvironment();
	
}

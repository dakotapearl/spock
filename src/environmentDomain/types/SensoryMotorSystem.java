package environmentDomain.types;

import environmentDomain.Action;
import environmentDomain.Environment;
import environmentDomain.EnvironmentDomain;
import environmentDomain.Perception;
import goalDomain.Goal;

import java.util.ArrayList;

public abstract class SensoryMotorSystem extends Environment {
	
	protected EnvironmentDomain environmentDomain;
	protected ArrayList<Action> actions;
	protected ArrayList<Perception> perceptions;
	protected ArrayList<Goal> goals;
	
	public SensoryMotorSystem(EnvironmentDomain environmentDomain) {
		super(environmentDomain);
		actions = new ArrayList<Action>();
		perceptions = new ArrayList<Perception>();
		goals = new ArrayList<Goal>();
	}
	
	public void setEnvironmentsDomain(EnvironmentDomain environmentDomain) {
		this.environmentDomain = environmentDomain;
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

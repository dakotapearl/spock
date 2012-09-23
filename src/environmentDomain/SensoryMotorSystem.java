package environmentsDomain;

import java.util.ArrayList;

import mechanismsDomain.ApplicationEventQueue;

public abstract class SensoryMotorSystem {
	
	protected EnvironmentsDomain environmentsDomain;
	protected ArrayList<Action> actions;
	protected ArrayList<Perception> perceptions;
	
	public SensoryMotorSystem() {
		actions = new ArrayList<Action>();
		perceptions = new ArrayList<Perception>();
	}
	
	public void setEnvironmentsDomain(EnvironmentsDomain environmentsDomain) {
		this.environmentsDomain = environmentsDomain;
	}
	
	public ArrayList<Action> getActions() {
		return actions;
	}
	
	public ArrayList<Perception> getPerceptions() {
		return perceptions;
	}
	
	public void addAction(Action a) {
		actions.add(a);
	}
	
	public void addPerception(Perception p) {
		perceptions.add(p);
	}
	
	public void makePerceptionsAccessible() {
		for (Perception p : perceptions) {
			if (p.isInitiallyAccessible())
				ApplicationEventQueue.generateEvent(Perception.MAKE_ACCESSIBLE_EVENT, null, p);
		}
	}
	
	public abstract void initialise();
	
}

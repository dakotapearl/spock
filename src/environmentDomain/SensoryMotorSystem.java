package environmentDomain;

import java.util.ArrayList;

public abstract class SensoryMotorSystem {
	
	protected EnvironmentDomain environmentDomain;
	protected ArrayList<Action> actions;
	protected ArrayList<Perception> perceptions;
	
	public SensoryMotorSystem() {
		actions = new ArrayList<Action>();
		perceptions = new ArrayList<Perception>();
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
	
	public void addAction(Action a) {
		actions.add(a);
	}
	
	public void addPerception(Perception p) {
		perceptions.add(p);
	}
	
	public void activatePerceptions() {
		for (Perception p : perceptions) {
			if (p.isInitiallyAccessible());
				//p.activate();
		}
	}
	
	public abstract void initialise();
	
}

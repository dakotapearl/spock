package metricDomain.goals.flavours;

import java.util.Observable;
import java.util.Observer;

import metricDomain.goals.Goal;


/**
 * Note: May be combined with GeneralGoal for better flexibility
 * @author Loren Chorley
 */
public abstract class NotifyGoal extends Observable implements Goal {

	protected void notifyRewardChange(double reward) {
		notifyObservers();
	}
	
    public void register(Observer o) {
		addObserver(o);
	}
	
	public void deregister(Observer o) {
		deleteObserver(o);
	}
	
}

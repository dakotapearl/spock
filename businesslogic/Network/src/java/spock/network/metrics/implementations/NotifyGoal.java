package spock.network.metrics.implementations;

import java.util.Observable;
import java.util.Observer;

import spock.network.metrics.GoalMetric;


/**
 * Note: May be combined with GeneralGoal for better flexibility
 * @author Loren Chorley
 */
public abstract class NotifyGoal extends Observable implements GoalMetric {

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

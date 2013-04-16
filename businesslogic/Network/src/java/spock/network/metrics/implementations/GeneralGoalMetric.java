package spock.network.metrics.implementations;

import spock.network.metrics.GoalMetric;


/**
 * @author Loren Chorley
 */
public interface GeneralGoalMetric extends GoalMetric {
	
	public double getReward();
	
}

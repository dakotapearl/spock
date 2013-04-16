package spock.network.metrics.implementations;


import java.util.Date;

import spock.network.metrics.GoalMetric;

/**
 * @author Loren Chorley
 */
public abstract class SpikeAndDecayGoal implements GoalMetric {

	protected double lastReward;
	protected Date lastTime;
	
    public void spike(double reward) {
    	lastTime = new Date();
    	lastReward = reward + getReward();
    }
    
    public abstract double getReward(); /*computes reward based on lastReward and lastTime*/
	
}

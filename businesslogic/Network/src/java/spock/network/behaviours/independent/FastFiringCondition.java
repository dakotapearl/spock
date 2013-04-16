package spock.network.behaviours.independent;

import spock.network.behaviours.FiringCondition;

/**
 *
 * @author Loren Chorley
 */
public class FastFiringCondition extends FiringCondition {

    public FastFiringCondition() {
        setReady();
    }

    @Override
    public FiringCondition replicate(FiringCondition parentBehaviour) {
        return new FastFiringCondition(); // Experiment with not changing it
    }

    @Override
    public void run() {}

    @Override
    public void refresh() {}

    @Override
    public void pauseActivity() {}

    @Override
    public void resumeActivity() {}
    
}

package spock.network.behaviours.isolatedImplementations;

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
    public FiringCondition replicate() {
        FiringCondition n;
        try {
            n = this.getClass().newInstance();
            return n; 
        } catch (InstantiationException e) {
            e.printStackTrace();
            System.exit(1);
        } catch (IllegalAccessException e) {
            e.printStackTrace();
            System.exit(1);
        } 
        return null;
    }

    @Override
    public void run() {
        // Empty
    }

    @Override
    public void refresh() {
        // Empty
    }
    
}

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package spockdataaccess.ejb;

import javax.ejb.Local;
import spockdataaccess.ejb.requestsupport.*;

/**
 *
 * @author Loren Chorley
 */
@Local
public interface RequestBeanLocal {
    public boolean login(String username, String passwordHash);
    public void sendPasswordResetEmail(String username);
    public ConfigurationFunctions Configuration();
    public NetworkFunctions Network();
    public ExperimentFunctions Experiment();
    public EnvironmentFunctions Environment();
    public UserFunctions User();
    public UserInterfaceFunctions UserInterface();
    public ConnectionFunctions Connection();
    public BehaviourFunctions Behaviour();
    public MetricFunctions Metric();
}

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package spockdataaccess.ejb;

import javax.ejb.Remote;
import spockdataaccess.ejb.requestsupport.*;

/**
 *
 * @author Loren Chorley
 */
@Remote
public interface RequestBeanRemote {
    public boolean login(String username, String passwordHash);
    public void sendPasswordResetEmail(String username);
    public ConfigurationFunctions getConfigurationFns();
    public NetworkFunctions getNetworkFns();
    public ExperimentFunctions getExperimentFns();
    public EnvironmentFunctions getEnvironmentFns();
    public UserFunctions getUserFns();
    public UserInterfaceFunctions getUserInterfaceFns();
    public ConnectionFunctions getConnectionFns();
    public BehaviourFunctions getBehaviourFns();
    public MetricFunctions getMetricFns();
}

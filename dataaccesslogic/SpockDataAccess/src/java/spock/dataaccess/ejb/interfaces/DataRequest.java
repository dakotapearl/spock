package spock.dataaccess.ejb.interfaces;

import javax.ejb.Remote;
import spock.dataaccess.ejb.interfaces.ExperimentFunctions;
import spock.dataaccess.ejb.interfaces.UserFunctions;

/**
 *
 * @author Loren Chorley
 */
@Remote
public interface DataRequest {
    public boolean login(String username, String passwordHash);
    public boolean userVerified();
    public void sendPasswordResetEmail(String username);
    public UserFunctions User();
    public ExperimentFunctions Experiment();
    public ConfigurationFunctions Configuration();
    public NetworkFunctions Network();
    public EnvironmentFunctions Environment();
    public UserInterfaceFunctions UserInterface();
    public ConnectionFunctions Connection();
    public BehaviourFunctions Behaviour();
    public MetricFunctions Metric();
    public String returnTestString();
}

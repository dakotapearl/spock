package spock.dataaccess.ejb.interfaces.entities;

import java.io.Serializable;
import java.util.Collection;

/**
 *
 * @author Loren Chorley
 */
public interface Environment extends Serializable {

    public String getId();
    public void setId(String id);
    public Boolean getIsActive();
    public void setIsActive(Boolean isActive);
    public Collection<Experiment> getExperiments();
    public void addExperiment(Experiment experiment);
    public String getCodeURL();
    public void setCodeURL(String CodeURL);
    public String getDataURL();
    public void setDataURL(String DataURL);
    public Collection<EnvironmentInterface> getEnvironmentInterfaces();
    public void setEnvironmentInterfaces(Collection<EnvironmentInterface> environmentInterfaces);
    public Collection<EnvironmentNode> getEnvironmentNodes();
    public void setEnvironmentNodes(Collection<EnvironmentNode> environmentNodes);
    public void addEnvironmentInterface(EnvironmentInterface environmentInterface);
    public void addEnvironmentNode(EnvironmentNode environmentNode);
    public Collection<Metric> getMetrics();
    public void setMetrics(Collection<Metric> metrics);
    
}

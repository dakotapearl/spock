package spock.dataaccess.entities;

import java.io.Serializable;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import spock.dataaccess.ejb.interfaces.entities.Configuration;

/**
 *
 * @author Loren Chorley
 */
@Entity
@Table(name = "Configurations")
public class ConfigurationEntity implements Configuration {
    private static final long serialVersionUID = 1L;

    @Id
    protected String name;

    public ConfigurationEntity() {
    }

    public ConfigurationEntity(String name, String configvalue) {
        this.name = name;
        this.configvalue = configvalue;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    protected String configvalue;

    public String getConfigValue() {
        return configvalue;
    }

    public void setConfigValue(String configvalue) {
        this.configvalue = configvalue;
    }

    
    @Override
    public int hashCode() {
        int hash = 0;
        hash += (name != null ? name.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof ConfigurationEntity)) {
            return false;
        }
        ConfigurationEntity other = (ConfigurationEntity) object;
        if ((this.name == null && other.name != null) || (this.name != null && !this.name.equals(other.name))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "spockdataaccess.entity.Config[ id=" + name + " ]";
    }
    
}
